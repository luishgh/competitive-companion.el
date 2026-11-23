// AtCoder ABC086A - Product: correct solution.
#include <iostream>

int main() {
  long long a, b;
  std::cin >> a >> b;
  std::cout << (a * b % 2 == 0 ? "Even" : "Odd") << '\n';
}
