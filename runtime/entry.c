extern void _fino_init_globals();
extern void _fino_entry();
int main() {
    _fino_init_globals();
    _fino_entry();
    return 0;
}
