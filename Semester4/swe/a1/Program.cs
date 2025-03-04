using System;
using System.Drawing;
using System.Reflection;
using System.Runtime.ConstrainedExecution;
using System.Runtime.InteropServices;

class Program
{
    static void Main(string[] args)
    {
            int n = 1001;
            int[,] matrix = new int[n+5,n+5];

            int i, j;
            i = n / 2 + 1;
            j = n / 2 + 1;
            int k = 1;

            matrix[i,j] = k;
            k++;
            j++;

            for (int r = 1; r <= n / 2; r++) {
                for (int p = 0; p < 2 * r; p++) {
                    matrix[i,j] = k;
                    k++;
                    i--;
                }
                i++;
                j--;

                for (int p = 0; p < 2 * r; p++) {
                    matrix[i,j] = k;
                    k++;
                    j--;
                }
                i--;
                j++;

                for (int p = 0; p < 2 * r; p++) {
                    matrix[i,j] = k;
                    k++;
                    i--;
                }
                i++;
                j++;

                for (int p = 0; p < 2 * r; p++) {
                    matrix[i,j] = k;
                    k++;
                    j++;
                }
            }

            for (int q = 1; q <= n; q++) {
                for (int w = 1; w < n; w++) {
                    Console.WriteLine(matrix[q, w]);
                }
                Console.WriteLine();
            }


    //     // Declare and initialize an array of integers
    //     int[] op1 = new int[1000];
    //     int[] op2 = new int[1000];
    //     int op1Length = 1;
    //     int op2Length = 0;

    //     // op1 * op2
    //     op1[0] = 1;

    //     for (int k = 1; k < 10; k++)
    //     {
    //         // put i in op2
    //         int x = k;
    //         int c = 0;
    //         while (x > 0)
    //         {
    //             op2[c] = x % 10;
    //             x /= 10;
    //             c++;
    //         }
    //         op2Length = c;


    //         // Multiply op1 and op2
    //         int carry = 0;
    //         for (int i = 0; i < op1Length; i++){
    //             for(int j = 0; j < op2Length; j++){
    //                 // it might have a carry
    //                 int m = op1[i] * op2[j];
    //                 op1[i] = m % 10 + carry;
    //                 carry = m / 10;
    //             }
    //         }



    //    }
    }
}