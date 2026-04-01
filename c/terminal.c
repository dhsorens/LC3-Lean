#include <lean/lean.h>
#include <termios.h>
#include <unistd.h>
#include <sys/select.h>

static struct termios orig_termios;
static int raw_mode_enabled = 0;

LEAN_EXPORT lean_obj_res lc3_enable_raw_mode(lean_obj_arg world) {
    if (!raw_mode_enabled) {
        tcgetattr(STDIN_FILENO, &orig_termios);
        struct termios raw = orig_termios;
        raw.c_lflag &= ~(ICANON | ECHO);
        raw.c_cc[VMIN] = 1;
        raw.c_cc[VTIME] = 0;
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
        raw_mode_enabled = 1;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lc3_disable_raw_mode(lean_obj_arg world) {
    if (raw_mode_enabled) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
        raw_mode_enabled = 0;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lc3_read_char(lean_obj_arg world) {
    uint8_t c = 0;
    read(STDIN_FILENO, &c, 1);
    return lean_io_result_mk_ok(lean_box((size_t)c));
}

LEAN_EXPORT lean_obj_res lc3_check_key(lean_obj_arg world) {
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);
    struct timeval timeout = {0, 0};
    int result = select(STDIN_FILENO + 1, &readfds, NULL, NULL, &timeout);
    return lean_io_result_mk_ok(lean_box(result > 0 ? 1 : 0));
}
