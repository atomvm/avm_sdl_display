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

#define CHAR_WIDTH 8
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
    uint16_t key;
    bool key_down;
};

enum primitive
{
    Invalid = 0,
    Image,
    Rect,
    Text
};

struct TextData
{
    Uint32 fgcolor;
    const char *text;
};

struct ImageData
{
    const char *pix;
};

struct BaseDisplayItem
{
    enum primitive primitive;
    int x;
    int y;
    int width;
    int height;
    uint32_t brcolor; /* bounding rect color */
    union
    {
        struct ImageData image_data;
        struct TextData text_data;
    } data;
};

typedef struct BaseDisplayItem BaseDisplayItem;

static term keyboard_pid;
static struct timespec ts0;
static int keyboard_event_fds[2];

static SDL_Surface *screen;
static pthread_mutex_t ready_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t ready = PTHREAD_COND_INITIALIZER;

static void consume_display_mailbox(Context *ctx);
static void *display_loop();

static inline Uint32 uint32_color_to_surface(SDL_Surface *screen, uint32_t color)
{
    return SDL_MapRGB(screen->format, (color >> 24) & 0xFF, (color >> 16) & 0xFF, (color >> 8) & 0xFF);
}

static int draw_image_x(int xpos, int ypos, int max_line_len, BaseDisplayItem *item)
{
    int x = item->x;
    int y = item->y;

    Uint32 bgcolor;
    bool visible_bg;
    if (item->brcolor != 0) {
        bgcolor = uint32_color_to_surface(screen, item->brcolor);
        visible_bg = true;
    } else {
        visible_bg = false;
    }

    int width = item->width;
    const char *data = item->data.image_data.pix;

    int drawn_pixels = 0;

    uint32_t *pixels = ((uint32_t *) data) + (ypos - y) * width + (xpos - x);
    Uint32 *pixmem32 = (Uint32 *) (((uint8_t *) screen->pixels) + screen->w * ypos * BPP + xpos * BPP);

    if (width > xpos - x + max_line_len) {
        width = xpos - x + max_line_len;
    }

    for (int j = xpos - x; j < width; j++) {
        uint32_t img_pixel = READ_32_UNALIGNED(pixels);
        if ((*pixels >> 24) & 0xFF) {
            Uint32 color = uint32_color_to_surface(screen, img_pixel);
            pixmem32[drawn_pixels] = color;
        } else if (visible_bg) {
            pixmem32[drawn_pixels] = bgcolor;
        } else {
            return drawn_pixels;
        }
        drawn_pixels++;
        pixels++;
    }

    return drawn_pixels;
}

static int draw_rect_x(int xpos, int ypos, int max_line_len, BaseDisplayItem *item)
{
    int x = item->x;
    int width = item->width;
    uint32_t color = uint32_color_to_surface(screen, item->brcolor);

    int drawn_pixels = 0;

    Uint32 *pixmem32 = (Uint32 *) (((uint8_t *) screen->pixels) + screen->w * ypos * BPP + xpos * BPP);

    if (width > xpos - x + max_line_len) {
        width = xpos - x + max_line_len;
    }

    for (int j = xpos - x; j < width; j++) {
        pixmem32[drawn_pixels] = color;
        drawn_pixels++;
    }

    return drawn_pixels;
}

static int draw_text_x(int xpos, int ypos, int max_line_len, BaseDisplayItem *item)
{
    int x = item->x;
    int y = item->y;
    Uint32 fgcolor = uint32_color_to_surface(screen, item->data.text_data.fgcolor);
    Uint32 bgcolor;
    bool visible_bg;
    if (item->brcolor != 0) {
        bgcolor = uint32_color_to_surface(screen, item->brcolor);
        visible_bg = true;
    } else {
        visible_bg = false;
    }

    char *text = (char *) item->data.text_data.text;

    int width = item->width;

    int drawn_pixels = 0;

    Uint32 *pixmem32 = (Uint32 *) (((uint8_t *) screen->pixels) + screen->w * ypos * BPP + xpos * BPP);

    if (width > xpos - x + max_line_len) {
        width = xpos - x + max_line_len;
    }

    for (int j = xpos - x; j < width; j++) {
        int char_index = j / CHAR_WIDTH;
        char c = text[char_index];
        unsigned const char *glyph = fontdata + ((unsigned char) c) * 16;

        unsigned char row = glyph[ypos - y];

        bool opaque;
        int k = j % CHAR_WIDTH;
        if (row & (1 << (7 - k))) {
            opaque = true;
        } else {
            opaque = false;
        }

        if (opaque) {
            pixmem32[drawn_pixels] = fgcolor;
        } else if (visible_bg) {
            pixmem32[drawn_pixels] = bgcolor;
        } else {
            return drawn_pixels;
        }
        drawn_pixels++;
    }

    return drawn_pixels;
}

static int find_max_line_len(BaseDisplayItem *items, int count, int xpos, int ypos)
{
    int line_len = screen->w;

    for (int i = 0; i < count; i++) {
        BaseDisplayItem *item = &items[i];

        if ((xpos < item->x) && (ypos >= item->y) && (ypos < item->y + item->height)) {
            int len_to_item = item->x - xpos;
            line_len = (line_len > len_to_item) ? len_to_item : line_len;
        }
    }

    return line_len;
}

static int draw_x(int xpos, int ypos, BaseDisplayItem *items, int items_count)
{
    bool below = false;

    for (int i = 0; i < items_count; i++) {
        BaseDisplayItem *item = &items[i];
        if ((xpos < item->x) || (xpos >= item->x + item->width) || (ypos < item->y) || (ypos >= item->y + item->height)) {
            continue;
        }

        int max_line_len = below ? 1 : find_max_line_len(items, i, xpos, ypos);

        int drawn_pixels = 0;
        switch (items[i].primitive) {
            case Image:
                drawn_pixels = draw_image_x(xpos, ypos, max_line_len, item);
                break;

            case Rect:
                drawn_pixels = draw_rect_x(xpos, ypos, max_line_len, item);
                break;

            case Text:
                drawn_pixels = draw_text_x(xpos, ypos, max_line_len, item);
                break;

            default: {
                fprintf(stderr, "unexpected display list command.\n");
            }
        }

        if (drawn_pixels != 0) {
            return drawn_pixels;
        }

        below = true;
    }

    return 1;
}

static void init_item(BaseDisplayItem *item, term req, Context *ctx)
{
    term cmd = term_get_tuple_element(req, 0);

    if (cmd == context_make_atom(ctx, "\x5"
                                      "image")) {
        item->primitive = Image;
        item->x = term_to_int(term_get_tuple_element(req, 1));
        item->y = term_to_int(term_get_tuple_element(req, 2));

        term bgcolor = term_get_tuple_element(req, 3);
        if (bgcolor == context_make_atom(ctx, "\xB"
                                              "transparent")) {
            item->brcolor = 0;
        } else {
            item->brcolor = ((uint32_t) term_to_int(bgcolor)) << 8 | 0xFF;
        }

        term img = term_get_tuple_element(req, 4);

        term format = term_get_tuple_element(img, 0);
        if (format != context_make_atom(ctx, "\x8"
                                             "rgba8888")) {
            fprintf(stderr, "unsupported image format: ");
            term_display(stderr, format, ctx);
            fprintf(stderr, "\n");
            return;
        }
        item->width = term_to_int(term_get_tuple_element(img, 1));
        item->height = term_to_int(term_get_tuple_element(img, 2));
        item->data.image_data.pix = term_binary_data(term_get_tuple_element(img, 3));

    } else if (cmd == context_make_atom(ctx, "\x4"
                                             "rect")) {
        item->primitive = Rect;
        item->x = term_to_int(term_get_tuple_element(req, 1));
        item->y = term_to_int(term_get_tuple_element(req, 2));
        item->width = term_to_int(term_get_tuple_element(req, 3));
        item->height = term_to_int(term_get_tuple_element(req, 4));
        item->brcolor = term_to_int(term_get_tuple_element(req, 5)) << 8 | 0xFF;

    } else if (cmd == context_make_atom(ctx, "\x4"
                                             "text")) {
        item->primitive = Text;
        item->x = term_to_int(term_get_tuple_element(req, 1));
        item->y = term_to_int(term_get_tuple_element(req, 2));

        item->data.text_data.fgcolor = term_to_int(term_get_tuple_element(req, 4));
        term bgcolor = term_get_tuple_element(req, 5);
        if (bgcolor == context_make_atom(ctx, "\xB"
                                              "transparent")) {
            item->brcolor = 0;
        } else {
            item->brcolor = ((uint32_t) term_to_int(bgcolor)) << 8 | 0xFF;
        }

        term font = term_get_tuple_element(req, 3);
        if (font != context_make_atom(ctx, "\xB"
                                           "default16px")) {
            fprintf(stderr, "unsupported font: ");
            term_display(stderr, font, ctx);
            fprintf(stderr, "\n");
            return;
        }

        term text_term = term_get_tuple_element(req, 6);
        int ok;
        item->data.text_data.text = interop_term_to_string(text_term, &ok);
        if (!ok) {
            fprintf(stderr, "invalid text.\n");
            return;
        }

        item->height = 16;
        item->width = strlen(item->data.text_data.text) * 8;

    } else {
        fprintf(stderr, "unexpected display list command: ");
        term_display(stderr, req, ctx);
        fprintf(stderr, "\n");

        item->primitive = Invalid;
        item->x = -1;
        item->y = -1;
        item->width = 1;
        item->height = 1;
    }
}

static void destroy_items(BaseDisplayItem *items, int items_count)
{
    for (int i = 0; i < items_count; i++) {
        BaseDisplayItem *item = &items[i];

        switch (item->primitive) {
            case Image:
                break;

            case Rect:
                break;

            case Text:
                free((char *) item->data.text_data.text);
                break;

            default: {
                break;
            }
        }
    }

    free(items);
}

static void do_update(Context *ctx, term display_list)
{
    int proper;
    int len = term_list_length(display_list, &proper);

    BaseDisplayItem *items = malloc(sizeof(BaseDisplayItem) * len);

    term t = display_list;
    for (int i = len - 1; i >= 0; i--) {
        init_item(&items[i], term_get_list_head(t), ctx);
        t = term_get_list_tail(t);
    }

    int screen_width = screen->w;
    int screen_height = screen->h;

    for (int ypos = 0; ypos < screen_height; ypos++) {
        int xpos = 0;
        while (xpos < screen_width) {
            int drawn_pixels = draw_x(xpos, ypos, items, len);
            xpos += drawn_pixels;
        }
    }

    destroy_items(items, len);
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
        do_update(ctx, display_list);

    } else if (cmd == context_make_atom(ctx, "\xF"
                                             "subscribe_input")) {
        keyboard_pid = pid;

    } else {
        fprintf(stderr, "unexpected command: ");
        term_display(stderr, req, ctx);
        fprintf(stderr, "\n");
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
    if (target) {
        mailbox_send(target, message);
    }
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

        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(3) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
            abort();
        }

        term up_down = keyb.key_down ? context_make_atom(ctx, "\x4"
                                                              "down")
                                     : context_make_atom(ctx, "\x2"
                                                              "up");

        term event_data_tuple = term_alloc_tuple(3, ctx);
        term_put_tuple_element(event_data_tuple, 0, context_make_atom(ctx, "\x8"
                                                                           "keyboard"));
        term_put_tuple_element(event_data_tuple, 1, up_down);
        term_put_tuple_element(event_data_tuple, 2, term_from_int(keyb.key));

        term event_tuple = term_alloc_tuple(4, ctx);
        term_put_tuple_element(event_tuple, 0, context_make_atom(ctx, "\xB"
                                                                      "input_event"));
        term_put_tuple_element(event_tuple, 1, term_from_local_process_id(ctx->process_id));
        term_put_tuple_element(event_tuple, 2, term_from_int(millis));
        term_put_tuple_element(event_tuple, 3, event_data_tuple);

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

    SDL_EnableUNICODE(1);

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
                if (event.key.keysym.unicode == 0) {
                    continue;
                } else if (event.key.keysym.unicode == '\r') {
                    event.key.keysym.unicode = '\n';
                }
                struct KeyboardEvent keyb_event;
                memset(&keyb_event, 0, sizeof(struct KeyboardEvent));
                keyb_event.key = event.key.keysym.unicode;
                keyb_event.key_down = true;
                write(keyboard_event_fds[1], &keyb_event, sizeof(keyb_event));
                break;
            }

            case SDL_KEYUP: {
                struct KeyboardEvent keyb_event;
                memset(&keyb_event, 0, sizeof(struct KeyboardEvent));
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
