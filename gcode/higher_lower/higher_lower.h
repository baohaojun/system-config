#ifndef __HIGHER_LOWER_H__
#define __HIGHER_LOWER_H__

/*macros*/
#define ITEM_HEIGHT 36
#define ICON_SIZE 32
#define LOOKING_FOR_HEIGHT 25
#define BUTTON_HEIGHT 36
#define HUNG_TIMEOUT 250

/*types*/
typedef struct {
    HWND wnd;
    HICON icon;
    wstring title_class;
    bool iconized;
    bool topmost;
} WINFO;


/*global declarations*/
extern vector<WINFO> v_winfo;
extern wstring looking_for;
extern unsigned int idx_selected;
extern unsigned int idx_first_visible;
extern RECT main_window_rect;
extern HWND main_window;
extern bool control_down;
extern bool start_as_daemon;
extern int up_button_top;
extern int down_button_top;
extern int first_item_top;
extern int last_item_bottom;
extern int maximun_shown_items;
extern int main_window_visible;

/*functions declarations*/
void update_found_list();
LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM);
bool window_match(wstring&);
BOOL CALLBACK EnumWindowsProc(HWND wnd, LPARAM lParam);
bool IsChildWnd(HWND wnd);
void draw_window(HWND wnd);
void rect_offset(RECT &rect, int x, int y);
int get_item_rect(RECT &rect, bool icon, unsigned int idx);
void get_looking_for_rect(RECT &rect);
void found_action(void);
void draw_mask_on_selected(HWND wnd, HDC hdc=NULL);
void change_selected(HWND wnd, int idx);
bool IsWindowSwitchable(HWND wnd);
void handle_wm_lbuttondown(WPARAM wParam, LPARAM lParam);
void handle_wm_lbuttondblclk(WPARAM wParam, LPARAM lParam);
int get_shown_item_idx_from_xy(LPARAM l);
void parse_command_line();
void hide_or_quit();
void restore_window(HWND wnd);
int number_of_visible_items();
void initialize_metrics();
void handle_mouse_wheel(WPARAM wParam, LPARAM lParam);
void draw_button(HDC hdc, int down);
bool my_get_window_module_file_name(HWND hwnd, wchar_t buff[], unsigned int siz);
void looking_for_foreground();
void tmp_foreput_selected();
void place_back_selected();
bool is_window_topmost(HWND hwnd);
void set_window_topmost(HWND hwnd, bool set);
#endif
