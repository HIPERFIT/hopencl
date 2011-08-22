#pragma OPENCL EXTENSION cl_amd_printf : enable

__kernel void vectorAdd(__global const float * a, __global const float * b, __global float * c) {
    /* Vector element index */
    int nIndex = get_global_id(0);
    c[nIndex] = a[nIndex] + b[nIndex];
    printf("Index: %d, a: %f, b: %f, c: %f\n", nIndex, a[nIndex], b[nIndex], c[nIndex]);
};
