#include <benchmark/benchmark.h>
#include <string>

std::string SomeFunction() { return std::string{"hello world, this is a big string just for testing..."}; }

static void BM_SomeFunction(benchmark::State& state)
{
    // Perform setup here
    for (auto _ : state)
    {
        // This code gets timed
        SomeFunction();
    }
}

BENCHMARK(BM_SomeFunction);
