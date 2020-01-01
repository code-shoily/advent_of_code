defmodule AdventOfCode.Y2018.Day2Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2018.Day2, as: Solution

  test "Word Count" do
    assert Solution.word_count("aabcacacdd") == %{"a" => 4, "b" => 1, "c" => 3, "d" => 2}
    assert Solution.word_count("") == %{}
    assert Solution.word_count("123") == %{"1" => 1, "2" => 1, "3" => 1}
  end

  test "Two or Threes" do
    assert Solution.two_or_three_count(%{"c" => 3, "d" => 2}) == %{two: 1, three: 1}
    assert Solution.two_or_three_count(%{"b" => 1, "d" => 2}) == %{two: 1, three: 0}
    assert Solution.two_or_three_count(%{"b" => 4, "d" => 3}) == %{two: 0, three: 1}
    assert Solution.two_or_three_count(%{"b" => 0, "d" => 0}) == %{two: 0, three: 0}
    assert Solution.two_or_three_count(%{}) == %{two: 0, three: 0}
  end

  test "Checksum 1" do
    data = [
      %{two: 1, three: 1},
      %{two: 0, three: 0},
      %{two: 0, three: 1},
      %{two: 1, three: 1}
    ]

    assert Solution.checksum_1(data) == 6
    assert Solution.checksum_1([%{two: 0, three: 0}]) == 0
  end

  test "Year 2018, Day 2, Part 1" do
    assert Solution.run_1() == 7221
  end

  test "Year 2018, Day 2, Part 2" do
    assert Solution.run_2() == "mkcdflathzwsvjxrevymbdpoq"
  end
end
