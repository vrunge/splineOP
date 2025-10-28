#ifndef SPLINEOPX_H
#define SPLINEOPX_H

#include <vector>

// Pure C++ dynamic programming function
std::vector<int> splineOP(const std::vector<double>& data,
                         double beta,
                          int S = 10,
                          int nb_initSpeed = 5,
                          double data_var = 1.0);

#endif // SPLINEOPX_H
