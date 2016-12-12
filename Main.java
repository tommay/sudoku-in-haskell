package net.tommay.spudoku;

import net.tommay.spudoku.CreaterForJava;

public class Main {
    public static void main (String[] args) {
        String puzzle = CreaterForJava.create(1, args[0]);
        System.out.println(puzzle);
    }
}
