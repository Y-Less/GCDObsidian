#include <cutil_inline.h>

__global__ void kernel1(float *input0,float *result0)
{
  int tidy = threadIdx.y;
  int bidx = blockIdx.x;
  int bidy = blockIdx.y;

  
  
  result0[((bidx * 32) + (((threadIdx.x * (gridDim.y * blockDim.y)) + ((bidy * blockDim.y) + tidy)) - (bidx * 32)))] = (((((input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? ((((threadIdx.x - 2) + (bidx * 32)) < 1024) ? (threadIdx.x - 2) : (1023 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 1024)))] * 0.17820324) + (input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? ((((threadIdx.x - 1) + (bidx * 32)) < 1024) ? (threadIdx.x - 1) : (1023 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 1024)))] * 0.21052226)) + (input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? (((threadIdx.x + (bidx * 32)) < 1024) ? threadIdx.x : (1023 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 1024)))] * 0.22254893)) + (input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? ((((threadIdx.x + 1) + (bidx * 32)) < 1024) ? (threadIdx.x + 1) : (1023 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 1024)))] * 0.21052226)) + (input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? ((((threadIdx.x + 2) + (bidx * 32)) < 1024) ? (threadIdx.x + 2) : (1023 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 1024)))] * 0.17820324));
  
}
extern "C" void
  DoRows3(float * smoothX, float * res)
{
  dim3
    dimBlocks(1024 / 32, 2048 / 32),
    dimThreads(32, 32);
  kernel1<<<dimBlocks, dimThreads>>>(smoothX, res);
}



__global__ void kernel2(float *input0,float *result0)
{
  int tidy = threadIdx.y;
  int bidx = blockIdx.x;
  int bidy = blockIdx.y;

  
  
  result0[((bidx * 32) + (((threadIdx.x * (gridDim.y * blockDim.y)) + ((bidy * blockDim.y) + tidy)) - (bidx * 32)))] = (((((input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? ((((threadIdx.x - 2) + (bidx * 32)) < 2048) ? (threadIdx.x - 2) : (2047 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 2048)))] * 0.17820324) + (input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? ((((threadIdx.x - 1) + (bidx * 32)) < 2048) ? (threadIdx.x - 1) : (2047 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 2048)))] * 0.21052226)) + (input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? (((threadIdx.x + (bidx * 32)) < 2048) ? threadIdx.x : (2047 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 2048)))] * 0.22254893)) + (input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? ((((threadIdx.x + 1) + (bidx * 32)) < 2048) ? (threadIdx.x + 1) : (2047 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 2048)))] * 0.21052226)) + (input0[((bidx * 32) + (((((int)threadIdx.x + (bidx * 32)) >= 0) ? ((((threadIdx.x + 2) + (bidx * 32)) < 2048) ? (threadIdx.x + 2) : (2047 - (bidx * 32))) : (0 - (bidx * 32))) + ((tidy + (bidy * blockDim.y)) * 2048)))] * 0.17820324));
  
}
}

extern "C" void
  DoCols3(float * smoothY, float * res)
{
  dim3
    dimBlocks(2048 / 32, 1024 / 32),
    dimThreads(32, 32);
  kernel2<<<dimBlocks, dimThreads>>>(smoothY, res);
}

