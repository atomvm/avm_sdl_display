cmake_minimum_required (VERSION 2.6)
project (simulator)

include_directories(${ATOMVM_INSTALL_PREFIX}/include/)

find_package(SDL)
include_directories(${SDL_INCLUDE_DIR})
set(EXTRA_LIBS )

if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -Wextra -ggdb")
endif()

set(CMAKE_SHARED_LIBRARY_PREFIX "")

add_library(avm_display_port_driver SHARED display.c uFontLib/ufontlib.c)
target_link_libraries(avm_display_port_driver ${SDL_LIBRARY})
set_property(TARGET avm_display_port_driver PROPERTY C_STANDARD 99)
set_property(TARGET avm_display_port_driver PROPERTY PREFIX "")
