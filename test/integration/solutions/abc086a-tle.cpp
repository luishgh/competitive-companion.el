// AtCoder ABC086A - Product: loops forever after reading the input.
#include <iostream>

int main() {
  long long a, b;
  std::cin >> a >> b;
  // volatile: an empty infinite loop may be optimized away.
  for (volatile unsigned long long i = 0;; i = i + 1) {
  }
}
