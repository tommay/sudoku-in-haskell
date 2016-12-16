package net.tommay.sudoku;

import net.tommay.sudoku.CreaterForJava;

import frege.prelude.PreludeBase;
import frege.prelude.PreludeBase.TTuple2;
import frege.run7.Thunk;

public class Main {
    public static void main (String[] args) {
        TTuple2<String,String> t = CreaterForJava.create(
            Thunk.<Integer>lazy(1), args[0]);
        String puzzle = PreludeBase.<String, String>fst(t);
        String solution = PreludeBase.<String, String>snd(t);
        System.out.println("Puzzle:   " + puzzle);
        System.out.println("Solution: " + solution);
    }
}
