// AtCoder ABC086A - Product: loops forever before reading its input,
// so a large input fills the pipe and writing it blocks.
int main() {
  for (volatile unsigned long long i = 0;; i = i + 1) {
  }
}
