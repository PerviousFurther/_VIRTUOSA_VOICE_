
#include "simple_exec_wrapper\thread_pool.h"

#include <iostream>

int w(char, int, float, double) {
  ::std::println(::std::cout, "See, there have lots of things here.");
  return 1;
}

int main() {
  using namespace grp;
  single_thread_executor e{};
  single_thread_executor e1{};
  auto c = just() | then([] {
             ::std::println(::std::cout, "Hello world.");
             return ::std::make_tuple('6', 5, 6.54f, 5.5);
           }) |
           on(e.get_scheduler(), then(w)) |
           on(e1.get_scheduler(), then([](int) {
                ::std::println(::std::cout, "Hello world from e1!");
                return "asgas";
              }));

  ::std::thread w{[c{::std::move(c)}]() mutable {
    try {
      ::std::println(::std::cout, "waited result! {}",
                     ::std::string(sync_wait(c)));
    } catch (::std::exception e) {
      ::std::println(::std::cout, "{}", ::std::string_view{e.what()});
    }
  }};

  //::std::println(::std::cout, "{}", typeid(c).name());
  ::std::this_thread::sleep_for(::std::chrono::milliseconds(500));
  e.run();
  e.wait();
  ::std::this_thread::sleep_for(::std::chrono::milliseconds(500));
  e1.run();
  e.wait();

  w.join();

  return 0;
}
