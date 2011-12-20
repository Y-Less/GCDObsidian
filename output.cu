// __global__ void kernel(float *input0,float *result0)
// {
  
  // extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  
  // sbase[threadIdx.x * blockDim.y + tidy]
    // = input0[bidx * 512 + (bidy * blockDim.y + tidy) * 512 * gridDim.x + threadIdx.x - 2] * 7.015932e-2
    // + input0[bidx * 512 + (bidy * blockDim.y + tidy) * 512 * gridDim.x + threadIdx.x - 2] * 0.13107488
    // + input0[bidx * 512 + (bidy * blockDim.y + tidy) * 512 * gridDim.x + threadIdx.x - 1] * 0.19071281
    // + input0[bidx * 512 + (bidy * blockDim.y + tidy) * 512 * gridDim.x + threadIdx.x + 0] * 0.21610592
    // + input0[bidx * 512 + (bidy * blockDim.y + tidy) * 512 * gridDim.x + threadIdx.x + 1] * 0.19071281
    // + input0[bidx * 512 + (bidy * blockDim.y + tidy) * 512 * gridDim.x + threadIdx.x + 2] * 0.13107488
    // + input0[bidx * 512 + (bidy * blockDim.y + tidy) * 512 * gridDim.x + threadIdx.x + 3] * 7.015932e-2;
  
  // __syncthreads();
  
  // result0[bidx * 512 + (bidy * blockDim.y + tidy) * 512 * gridDim.x + threadIdx.x]
    // = sbase[threadIdx.x * blockDim.y + tidy - 3] * 7.015932e-2
    // + sbase[threadIdx.x * blockDim.y + tidy - 2] * 0.13107488
    // + sbase[threadIdx.x * blockDim.y + tidy - 1] * 0.19071281
    // + sbase[threadIdx.x * blockDim.y + tidy + 0] * 0.21610592
    // + sbase[threadIdx.x * blockDim.y + tidy + 1] * 0.19071281
    // + sbase[threadIdx.x * blockDim.y + tidy + 2] * 0.13107488
    // + sbase[threadIdx.x * blockDim.y + tidy + 3] * 7.015932e-2;
  
// }



// dy = tidy - 5 + bidy * 512
// dx = threadIdx.x - 5 + bidx * 512

// __global__ void kernel(float *input0,float *result0)
// {
  
  
  // extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  
  
  // sbase[tidy * 512 + threadIdx.x]
    // = input0[
      // bidx * 512
      // + (bidy * blockDim.y + (dy >= 0 ? (dy < 512 ? dy : 511) : 0)) * 512 * gridDim.x
      // + (dx >= 0 ? (dx < 512 ? dx : 511) : 0)];
  
  
  // __syncthreads();
  
  
  // result0[bidx*512 + threadIdx.x] = sbase[threadIdx.x];
  
  
// }


// dy = ((tidy - 3) + (bidy * 512))
// dx = ((threadIdx.x - 3) + (bidx * 512))

// __global__ void kernel(float *input0,float *result0){


  // result0[bidx*1024+tidy*512+threadIdx.x]
    // = input0[
      // bidx * 512
      // + (bidy * blockDim.y + (dy >= 0 ? (dy < 1024 ? dy : 1023) : 0)) * 512 * gridDim.x
      // + (dx >= 0 ? (dx < 2048 ? (threadIdx.x - 3) : 2047) : 0)];
  // result0[bidx*1024+tidy*512+threadIdx.x] = input0[((bidx*512)+(((((bidy*blockDim.y)+((((tidy-3)+(bidy*512))>=0) ? ((((tidy-3)+(bidy*512))<1024) ? ((tidy-3)+(bidy*512)) : 1023) : 0))*512)*gridDim.x)+((((threadIdx.x+509)+(bidx*512))>=0) ? ((((threadIdx.x+509)+(bidx*512))<2048) ? (threadIdx.x+509) : 2047) : 0)))];
  // result0[bidx*1024+tidy*512+threadIdx.x] = input0[((bidx*512)+(((((bidy*blockDim.y)+((((tidy+509)+(bidy*512))>=0) ? ((((tidy+509)+(bidy*512))<1024) ? ((tidy+509)+(bidy*512)) : 1023) : 0))*512)*gridDim.x)+((((threadIdx.x-3)+(bidx*512))>=0) ? ((((threadIdx.x-3)+(bidx*512))<2048) ? (threadIdx.x-3) : 2047) : 0)))];
  // result0[bidx*1024+tidy*512+threadIdx.x] = input0[((bidx*512)+(((((bidy*blockDim.y)+((((tidy+509)+(bidy*512))>=0) ? ((((tidy+509)+(bidy*512))<1024) ? ((tidy+509)+(bidy*512)) : 1023) : 0))*512)*gridDim.x)+((((threadIdx.x+509)+(bidx*512))>=0) ? ((((threadIdx.x+509)+(bidx*512))<2048) ? (threadIdx.x+509) : 2047) : 0)))];

// }




// __global__ void kernel(float *input0,float *result0){

  // extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  
  // sbase[tidy * 2 * 2 + threadIdx.x]
    // = input0[
      // bidx * 2
      // + ((threadIdx.x - 1 + bidx * 2 >= 0) ? ((threadIdx.x - 1 + bidx * 2 < 6) ? (threadIdx.x - 1) : (5 - bidx * 2)) : (0 - bidx * 2))
      // + ((tidy - 1 + bidy * 2 >= 0) ? ((tidy - 1 + bidy * 2 < 6) ? (tidy - 1 + bidy * 2) : 5) : 0) * 2 * gridDim.x];
  
  
  // (( float *)sbase)[((((tidy*2)*2)+threadIdx.x)+2)] = input0[((bidx*2)+(((((threadIdx.x-1)+(bidx*2))>=0) ? ((((threadIdx.x-1)+(bidx*2))<4) ? (threadIdx.x-1) : (3-(bidx*2))) : 0)+((((((tidy-3)+(bidy*2)
// )>=0) ? ((((tidy-3)+(bidy*2))<4) ? ((tidy-3)+(bidy*2)) : 3) : 0)*2)*gridDim.x)))];
  // (( float *)sbase)[((((tidy*2)*2)+threadIdx.x)+((2*2)*2))] = input0[((bidx*2)+(((((threadIdx.x-3)+(bidx*2))>=0) ? ((((threadIdx.x-3)+(bidx*2))<4) ? (threadIdx.x-3) : (3-(bidx*2))) : 0)+((((((tidy-1)+
// (bidy*2))>=0) ? ((((tidy-1)+(bidy*2))<4) ? ((tidy-1)+(bidy*2)) : 3) : 0)*2)*gridDim.x)))];
  
  
  // sbase[tidy * 2 * 2 + threadIdx.x + 2 * 2 * 2 + 2]
    // = input0[
      // bidx * 2
      // + ((threadIdx.x + 1 + bidx * 2 >= 0) ? ((threadIdx.x + 1 + bidx * 2 < 6) ? (threadIdx.x + 1) : (5 - bidx * 2)) : 0)
      // + ((tidy + 1 + bidy * 2 >= 0) ? ((tidy + 1 + bidy * 2 < 6) ? (tidy + 1 + bidy * 2) : 5) : 0) * 2 * gridDim.x];


  // __syncthreads();
  
  
  // result0[((bidx*4)+threadIdx.x)] = (( float *)sbase)[threadIdx.x];
  // result0[((bidx*4)+(threadIdx.x+2))] = (( float *)sbase)[(threadIdx.x+2)];

// }

// __global__ void kernel(float *input0,float *result0)
// {
  
  // extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  
  // sbase[tidy * 2 * 10 + threadIdx.x] = input0[(bidx * 10) + ((((threadIdx.x - 3) + (bidx * 10)) >= 0) ? ((((threadIdx.x - 3) + (bidx * 10)) < 40) ? (threadIdx.x - 3) : (39 - (bidx * 10))) : (0 - (bidx * 10))) + ((((((tidy - 3) + (bidy * 10)) >= 0) ? ((((tidy - 3) + (bidy * 10)) < 40) ? ((tidy - 3) + (bidy * 10)) : 39) : 0) * 10) * gridDim.x)];
  
  // sbase[tidy * 2 * 10 + threadIdx.x + 10] = input0[((bidx * 10) + (((((threadIdx.x + 7) + (bidx * 10)) >= 0) ? ((((threadIdx.x + 7) + (bidx * 10)) < 40) ? (threadIdx.x + 7) : (39 - (bidx * 10))) : (0 - (bidx * 10))) + ((((((tidy - 3) + (bidy * 10)) >= 0) ? ((((tidy - 3) + (bidy * 10)) < 40) ? ((tidy - 3) + (bidy * 10)) : 39) : 0) * 10) * gridDim.x)))];
  
  // sbase[tidy * 2 * 10 + threadIdx.x + 10 * 10 * 2] = input0[((bidx * 10) + (((((threadIdx.x - 3) + (bidx * 10)) >= 0) ? ((((threadIdx.x - 3) + (bidx * 10)) < 40) ? (threadIdx.x - 3) : (39 - (bidx * 10))) : (0 - (bidx * 10))) + ((((((tidy + 7) + (bidy * 10)) >= 0) ? ((((tidy + 7) + (bidy * 10)) < 40) ? ((tidy + 7) + (bidy * 10)) : 39) : 0) * 10) * gridDim.x)))];
  
  // sbase[tidy * 2 * 10 + threadIdx.x + 10 * 10 * 2 + 10] = input0[(bidx * 10) + ((((threadIdx.x + 7) + (bidx * 10)) >= 0) ? ((((threadIdx.x + 7) + (bidx * 10)) < 40) ? (threadIdx.x + 7) : (39 - (bidx * 10))) : (0 - (bidx * 10))) + ((((((tidy + 7) + (bidy * 10)) >= 0) ? ((((tidy + 7) + (bidy * 10)) < 40) ? ((tidy + 7) + (bidy * 10)) : 39) : 0) * 10) * gridDim.x)];
  
  // __syncthreads();
  
  // result0[((bidx * 20) + threadIdx.x)] = (( float *)sbase)[threadIdx.x];
  // result0[((bidx * 20) + (threadIdx.x + 10))] = (( float *)sbase)[(threadIdx.x + 10)];
  
// }



// __global__ void kernel(float *input0,float *result0)
// {
  
  // extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  // (( float *)sbase)[threadIdx.x] = input0[((bidx * 10) + threadIdx.x)];
  // __syncthreads();
  
  
  
  // (( float *)(sbase + 40))[(((threadIdx.x + (2 * 10)) * blockDim.y) + tidy)]
    // = sbase[((tidy * 10) + (threadIdx.x + (0 / 2))) - 3] * 7.015932e-2
    // + sbase[((tidy * 10) + (threadIdx.x + (0 / 2))) - 2] * 0.13107488
    // + sbase[((tidy * 10) + (threadIdx.x + (0 / 2))) - 1] * 0.19071281
    // + sbase[((tidy * 10) + (threadIdx.x + (0 / 2))) + 0] * 0.21610592
    // + sbase[((tidy * 10) + (threadIdx.x + (0 / 2))) + 1] * 0.19071281
    // + sbase[((tidy * 10) + (threadIdx.x + (0 / 2))) + 2] * 0.13107488
    // + sbase[((tidy * 10) + (threadIdx.x + (0 / 2))) + 3] * 7.015932e-2;
  
  
  
  
  
  // (( float *)(sbase + 40))[(((threadIdx.x + (2 * 10)) * blockDim.y) + (tidy + 10))]
    // = sbase[((tidy * 10) + (threadIdx.x + (10 / 2))) - 3] * 7.015932e-2)
    // + sbase[((tidy * 10) + (threadIdx.x + (10 / 2))) - 2] * 0.13107488)
    // + sbase[((tidy * 10) + (threadIdx.x + (10 / 2))) - 1] * 0.19071281)
    // + sbase[((tidy * 10) + (threadIdx.x + (10 / 2))) + 0] * 0.21610592)
    // + sbase[((tidy * 10) + (threadIdx.x + (10 / 2))) + 1] * 0.19071281)
    // + sbase[((tidy * 10) + (threadIdx.x + (10 / 2))) + 2] * 0.13107488)
    // + sbase[((tidy * 10) + (threadIdx.x + (10 / 2))) + 3] * 7.015932e-2);
  
  
  
  
  // __syncthreads();
  
  
  
  // result0[((bidx * 20) + threadIdx.x)] = (( float *)(sbase+40))[threadIdx.x];
  // result0[((bidx * 20) + (threadIdx.x + 10))] = (( float *)(sbase+40))[(threadIdx.x + 10)];
  
// }




















