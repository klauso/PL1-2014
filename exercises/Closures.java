public class Closures {
    static class Box {
        int content = 42;
    }

    public static Object helper() {
        final Box box = new Box();

        Object closure = new Object() {
                public String toString() {
                    return Integer.toString(box.content);
                }
        };

        box.content = 27;

        return closure;
    }

    public static void main(String[] args) {        
        System.out.println(helper().toString());
    }
}
