package search;

import java.util.*;
import java.lang.*;

public class BinarySearchMissing {
    public static void main(String[] args) {
        int x = Integer.parseInt(args[0]);
        ArrayList<Integer> data = new ArrayList<>();
        for (int i = 1; i < args.length; i++) {
            data.add(Integer.parseInt(args[i]));
        }

        System.out.println(BinarySearchMissing.cycledLowerBound(data, x));
        //System.out.println(BinarySearchMissing.recursiveLowerBound(data, x, -1, data.size()));
    }

    // pre: a - sorted in non-ascending order
    // post: pos of elem in array A or insertion position if there is no elem
    public static int cycledLowerBound(ArrayList<Integer> a, int elem) {
        int L = -1, R = a.size();
        // I: (a[L] > elem  || L == -1) and (a[R] <= elem || R == a.size())
        while (R - L > 1) {
            // I and R - L >= 2
            int M = L + (R - L) / 2;
            // M > L and M < R
            if (a.get(M) > elem) {
                // a[M] > elem
                L = M;
                // a[L] > elem
            } else {
                // a[M] <= elem
                R = M;
                // a[R] <= elem
            }
            // I
        }

        // I
        if (R == a.size() || a.get(R) != elem) {
            // a[L] > elem and a[R] < elem and L = R - 1 -> no element
            return (-(R) - 1);
        }
        // a[L] > elem and a[R] <= elem and L = R - 1 -> answer is R
        return R;

        // post: R is the least pos where a[R] == elem or insertionPoint if there is no elem in an array
    }

    // pre: a - sorted in non-ascending order, L < R
    // (L == -1 || a[L] > elem) and (R == a.size() || a[R] <= elem)
    // post: pos of elem in array A or insertion position if there is no elem
    public static int recursiveLowerBound(ArrayList<Integer> a, int elem, int L, int R) {
        if (R - L == 1) {
            // L == R - 1
            if (R == a.size() || a.get(R) != elem) {
                // a[L] > elem and a[R] < elem and L = R - 1 -> no element
                return (-(R) - 1);
            }
            // a[L] > elem and a[R] <= elem and L = R - 1 -> answer is R
            return R;
            // post: R is the least pos where a[R] == elem or insertionPoint if there is no elem in an array
        } else {
            int M = L + (R - L) / 2;
            // M > L and M < R
            if (a.get(M) > elem) {
                // I and a[M] > elem
                // (a[M] > elem) and (a[R] <= elem || R == a.size()) == I for M, R
                return recursiveLowerBound(a, elem, M, R);
            } else {
                // I and a[M] <= elem
                // (a[L] > elem  || L == -1) and (a[M] <= elem) == I for L, M
                return recursiveLowerBound(a, elem, L, M);
            }
        }
    }
}
