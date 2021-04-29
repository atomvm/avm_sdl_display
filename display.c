#include <SDL.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

#include <AtomVM/context.h>
#include <AtomVM/defaultatoms.h>
#include <AtomVM/interop.h>
#include <AtomVM/mailbox.h>
#include <AtomVM/term.h>
#include <AtomVM/utils.h>

#define SCREEN_WIDTH 320
#define SCREEN_HEIGHT 240
#define BPP 4
#define DEPTH 32

#include "font.c"

// from AtomVM generic_unix_sys.h

typedef struct EventListener EventListener;

typedef void (*event_handler_t)(EventListener *listener);

struct DisplayOpts
{
    avm_int_t width;
    avm_int_t height;
};

struct EventListener
{
    struct ListHead listeners_list_head;

    event_handler_t handler;
    void *data;
    int fd;
};

struct GenericUnixPlatformData
{
    struct ListHead *listeners;
};

// end of generic_unix_sys.h private header

struct KeyboardEvent
{
    uint8_t key;
    bool key_down;
};

static term keyboard_pid;
static struct timespec ts0;
static int keyboard_event_fds[2];

static SDL_Surface *screen;
static pthread_mutex_t ready_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t ready = PTHREAD_COND_INITIALIZER;

static void consume_display_mailbox(Context *ctx);
static void *display_loop();

static void draw_image(SDL_Surface *screen, int x, int y, int width, int height, const char *data, Uint8 r, Uint8 g, Uint8 b)
{
    Uint32 background_color = SDL_MapRGB(screen->format, r, g, b);
    uint32_t *pixels = (uint32_t *) data;

    for (int i = 0; i < height; i++) {
        Uint32 *pixmem32 = (Uint32 *) (((uint8_t *) screen->pixels) + (screen->w * (y + i) + x) * BPP);
        for (int j = 0; j < width; j++) {
            Uint32 color;
            if ((*pixels >> 24) & 0xFF) {
                color = SDL_MapRGB(screen->format, (*pixels) & 0xFF, (*pixels >> 8) & 0xFF, (*pixels >> 16) & 0xFF);
            } else {
                color = background_color;
            }
            pixmem32[j] = color;
            pixels++;
        }
    }
}

static void draw_rect(SDL_Surface *screen, int x, int y, int width, int height, Uint8 r, Uint8 g, Uint8 b)
{
    Uint32 color = SDL_MapRGB(screen->format, r, g, b);

    for (int i = 0; i < height; i++) {
        Uint32 *pixmem32 = (Uint32 *) (((uint8_t *) screen->pixels) + (screen->w * (y + i) + x) * BPP);
        for (int j = 0; j < width; j++) {
            pixmem32[j] = color;
        }
    }
}

static void draw_text(SDL_Surface *screen, int x, int y, const char *text, Uint8 r, Uint8 g, Uint8 b, Uint8 bgr, Uint8 bgg, Uint8 bgb)
{
    int len = strlen(text);
    Uint32 fgcolor = SDL_MapRGB(screen->format, r, g, b);
    Uint32 bgcolor = SDL_MapRGB(screen->format, bgr, bgg, bgb);

    for (int i = 0; i < len; i++) {
        unsigned const char *glyph = fontdata + ((unsigned char) text[i]) * 16;

        for (int j = 0; j < 16; j++) {
            unsigned char row = glyph[j];

            Uint32 *pixmem32 = (Uint32 *) (((uint8_t *) screen->pixels) + (screen->w * (y + j) + x + i * 8) * BPP);
            for (int k = 0; k < 8; k++) {
                if (row & (1 << (7 - k))) {
                    pixmem32[k] = fgcolor;
                } else {
                    pixmem32[k] = bgcolor;
                }
            }
        }
    }
}

static void execute_command(Context *ctx, term req)
{
    term cmd = term_get_tuple_element(req, 0);

    if (cmd == context_make_atom(ctx, "\xA"
                                      "draw_image")
        || cmd == context_make_atom(ctx, "\x5"
                                         "image")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        uint32_t bgcolor = term_to_int(term_get_tuple_element(req, 3));
        term img = term_get_tuple_element(req, 4);

        term format = term_get_tuple_element(img, 0);
        if (format != context_make_atom(ctx, "\x8"
                                             "rgba8888")) {
            fprintf(stderr, "unsupported image format: ");
            term_display(stderr, format, ctx);
            fprintf(stderr, "\n");
            return;
        }
        int width = term_to_int(term_get_tuple_element(img, 1));
        int height = term_to_int(term_get_tuple_element(img, 2));
        const char *data = term_binary_data(term_get_tuple_element(img, 3));

        draw_image(screen, x, y, width, height, data, (bgcolor >> 16),
            (bgcolor >> 8) & 0xFF, bgcolor & 0xFF);

    } else if (cmd == context_make_atom(ctx, "\x9"
                                             "draw_rect")
        || cmd == context_make_atom(ctx, "\x4"
                                         "rect")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        int width = term_to_int(term_get_tuple_element(req, 3));
        int height = term_to_int(term_get_tuple_element(req, 4));
        int color = term_to_int(term_get_tuple_element(req, 5));

        draw_rect(screen, x, y, width, height,
            (color >> 16) & 0xFF, (color >> 8) & 0xFF, color & 0xFF);

    } else if (cmd == context_make_atom(ctx, "\x9"
                                             "draw_text")
        || cmd == context_make_atom(ctx, "\x4"
                                         "text")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        term font = term_get_tuple_element(req, 3);
        uint32_t fgcolor = term_to_int(term_get_tuple_element(req, 4));
        uint32_t bgcolor = term_to_int(term_get_tuple_element(req, 5));
        term text_term = term_get_tuple_element(req, 6);

        if (font != context_make_atom(ctx, "\xB"
                                           "default16px")) {
            fprintf(stderr, "unsupported font: ");
            term_display(stderr, font, ctx);
            fprintf(stderr, "\n");
            return;
        }

        int ok;
        char *text = interop_term_to_string(text_term, &ok);

        draw_text(screen, x, y, text, (fgcolor >> 16) & 0xFF, (fgcolor >> 8) & 0xFF, fgcolor & 0xFF,
            (bgcolor >> 16) & 0xFF, (bgcolor >> 8) & 0xFF, bgcolor & 0xFF);

        free(text);

    } else if (cmd == context_make_atom(ctx, "\x6"
                                             "listen")) {
        keyboard_pid = term_get_tuple_element(req, 1);

    } else {
        fprintf(stderr, "display: ");
        term_display(stderr, req, ctx);
        fprintf(stderr, "\n");
    }
}

static void execute_commands(Context *ctx, term display_list)
{
    term t = display_list;

    while (term_is_nonempty_list(t)) {
        execute_command(ctx, term_get_list_head(t));
        t = term_get_list_tail(t);
    }
}

static void process_message(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;

    term pid = term_get_tuple_element(msg, 1);
    term ref = term_get_tuple_element(msg, 2);
    term req = term_get_tuple_element(msg, 3);

    term cmd = term_get_tuple_element(req, 0);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    if (SDL_MUSTLOCK(screen)) {
        if (SDL_LockSurface(screen) < 0) {
            return;
        }
    }

    if (cmd == context_make_atom(ctx, "\x6"
                                      "update")) {
        term display_list = term_get_tuple_element(req, 1);
        execute_commands(ctx, display_list);

    } else if (cmd == context_make_atom(ctx, "\xC"
                                             "clear_screen")) {
        int color = term_to_int(term_get_tuple_element(req, 1));

        struct DisplayOpts *disp_opts = ctx->platform_data;
        draw_rect(screen, 0, 0, disp_opts->width, disp_opts->height,
            (color >> 16) & 0xFF, (color >> 8) & 0xFF, color & 0xFF);

    } else {
        execute_command(ctx, req);
    }

    if (SDL_MUSTLOCK(screen)) {
        SDL_UnlockSurface(screen);
    }

    SDL_Flip(screen);

    if (UNLIKELY(memory_ensure_free(ctx, REF_SIZE + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        abort();
    }

    term return_tuple = term_alloc_tuple(2, ctx);

    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    mailbox_send(target, return_tuple);

    free(message);
}

static void consume_display_mailbox(Context *ctx)
{
    while (!list_is_empty(&ctx->mailbox)) {
        process_message(ctx);
    }
}

static void send_message(term pid, term message, GlobalContext *global)
{
    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(global, local_process_id);
    mailbox_send(target, message);
}

static void keyboard_callback(EventListener *listener)
{
    Context *ctx = (Context *) listener->data;

    struct KeyboardEvent keyb;
    read(keyboard_event_fds[0], &keyb, sizeof(keyb));

    if (keyboard_pid) {
        struct timespec ts;
        clock_gettime(CLOCK_MONOTONIC, &ts);

        avm_int_t millis = (ts.tv_sec - ts0.tv_sec) * 1000 + (ts.tv_nsec - ts0.tv_nsec) / 1000000;

        if (UNLIKELY(memory_ensure_free(ctx, 5) != MEMORY_GC_OK)) {
            abort();
        }

        term event_tuple = term_alloc_tuple(4, ctx);
        term_put_tuple_element(event_tuple, 0, context_make_atom(ctx, "\xE"
                                                                      "keyboard_event"));
        term_put_tuple_element(event_tuple, 1, term_from_int(keyb.key));
        term_put_tuple_element(event_tuple, 2, keyb.key_down ? TRUE_ATOM : FALSE_ATOM);
        term_put_tuple_element(event_tuple, 3, term_from_int(millis));

        send_message(keyboard_pid, event_tuple, ctx->global);
    }
}

void display_port_driver_init(Context *ctx, term opts)
{
    ctx->native_handler = consume_display_mailbox;

    term width_atom = context_make_atom(ctx, "\x5"
                                             "width");
    term height_atom = context_make_atom(ctx, "\x6"
                                              "height");

    term width_term = interop_proplist_get_value_default(opts, width_atom, term_from_int(SCREEN_WIDTH));
    term height_term = interop_proplist_get_value_default(opts, height_atom, term_from_int(SCREEN_HEIGHT));

    avm_int_t width = term_to_int(width_term);
    avm_int_t height = term_to_int(height_term);

    struct DisplayOpts *disp_opts = malloc(sizeof(struct DisplayOpts));
    if (IS_NULL_PTR(disp_opts)) {
        abort();
    }
    disp_opts->width = width;
    disp_opts->height = height;
    ctx->platform_data = disp_opts;

    pipe(keyboard_event_fds);

    UNUSED(opts);

    pthread_t thread_id;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_create(&thread_id, &attr, display_loop, disp_opts);

    pthread_mutex_lock(&ready_mutex);
    pthread_cond_wait(&ready, &ready_mutex);
    pthread_mutex_unlock(&ready_mutex);

    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    listener->fd = keyboard_event_fds[0];
    listener->data = ctx;
    listener->handler = keyboard_callback;
    linkedlist_append(&platform->listeners, &listener->listeners_list_head);

    clock_gettime(CLOCK_MONOTONIC, &ts0);
}

void *display_loop(void *args)
{
    struct DisplayOpts *disp_opts = (struct DisplayOpts *) args;

    pthread_mutex_lock(&ready_mutex);

    UNUSED(args);

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        abort();
    }

    if (!(screen = SDL_SetVideoMode(disp_opts->width, disp_opts->height, DEPTH, SDL_HWSURFACE))) {
        SDL_Quit();
        abort();
    }

    if (SDL_MUSTLOCK(screen)) {
        if (SDL_LockSurface(screen) < 0) {
            return NULL;
        }
    }

    memset(screen->pixels, 0x80, disp_opts->width * disp_opts->height * BPP);

    if (SDL_MUSTLOCK(screen)) {
        SDL_UnlockSurface(screen);
    }

    SDL_Flip(screen);

    pthread_cond_signal(&ready);
    pthread_mutex_unlock(&ready_mutex);

    SDL_Event event;

    while (SDL_WaitEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT: {
                exit(EXIT_SUCCESS);
                break;
            }

            case SDL_KEYDOWN: {
                struct KeyboardEvent keyb_event;
                keyb_event.key = event.key.keysym.sym;
                keyb_event.key_down = true;
                write(keyboard_event_fds[1], &keyb_event, sizeof(keyb_event));
                break;
            }

            case SDL_KEYUP: {
                struct KeyboardEvent keyb_event;
                keyb_event.key = event.key.keysym.sym;
                keyb_event.key_down = false;
                write(keyboard_event_fds[1], &keyb_event, sizeof(keyb_event));
                break;
            }

            default: {
                break;
            }
        }
    }

    return NULL;
}
