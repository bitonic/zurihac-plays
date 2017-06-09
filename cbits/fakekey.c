////////////////////////////////////////////////////////////////////////////////
#ifdef __linux__

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/extensions/XTest.h>

Display *fakekey_dis;

void fakekey_init(void) {
    fakekey_dis = XOpenDisplay(NULL);
}

void fakekey_press(int code) {
    XTestFakeKeyEvent(fakekey_dis, code, True, 0);
    XFlush(fakekey_dis);
}

void fakekey_release(int code) {
    XTestFakeKeyEvent(fakekey_dis, code, False, 0);
    XFlush(fakekey_dis);
}

#else

void fakekey_init(void) {
}

void fakekey_press(int code) {
}

void fakekey_release(int code) {
}

#endif
