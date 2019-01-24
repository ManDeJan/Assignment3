class Hello
{   
    void main()
    {
        int i;
        i = 0;
        while (false) {
            i = i + 5;
        }
        test();
    }

    int j;

    void test1() {
        int i;
        for (i = 1; i < 5; i + 1) {
            i = i * 2;
        }
    }

    void test2() {
        int i;
        i = 1;
        while (i < 5) {
            i = i * 2;
            i + 1;
        }
    }
}
