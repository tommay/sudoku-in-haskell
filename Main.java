package net.tommay.spudoku;

import net.tommay.spudoku.CreaterForJava;

import frege.prelude.PreludeBase.TTuple2;
import frege.runtime.Delayed;

public class Main {
    public static void main (String[] args) {
        TTuple2 t = CreaterForJava.create(1, args[0]);
        String puzzle = Delayed.<String>forced(t.mem1);
        String solution = Delayed.<String>forced(t.mem2);
        System.out.println("Puzzle:   " + puzzle);
        System.out.println("Solution: " + solution);
    }
}
