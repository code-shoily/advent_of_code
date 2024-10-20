defmodule AdventOfCode.Y2018.Day02Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1802

  alias AdventOfCode.Y2018.Day02, as: Solution

  test "Letter Count" do
    assert Solution.letter_count("aabcacacdd") == %{"a" => 4, "b" => 1, "c" => 3, "d" => 2}
    assert Solution.letter_count("") == %{}
    assert Solution.letter_count("123") == %{"1" => 1, "2" => 1, "3" => 1}
  end

  test "Two or Threes" do
    assert Solution.two_or_three_count(%{"c" => 3, "d" => 2}) == {1, 1}
    assert Solution.two_or_three_count(%{"b" => 1, "d" => 2}) == {1, 0}
    assert Solution.two_or_three_count(%{"b" => 4, "d" => 3}) == {0, 1}
    assert Solution.two_or_three_count(%{"b" => 0, "d" => 0}) == {0, 0}
    assert Solution.two_or_three_count(%{}) == {0, 0}
  end

  test "Checksum 1" do
    data = [{1, 1}, {0, 0}, {0, 1}, {1, 1}]

    assert Solution.checksum(data) == 6
    assert Solution.checksum([{0, 0}]) == 0
  end

  test "Remove at" do
    assert Solution.remove_at("abc", 0) == "?bc"
    assert Solution.remove_at("abc", 1) == "a?c"
    assert Solution.remove_at("abc", 2) == "ab?"
  end

  test "Remove one char" do
    assert Solution.words_without_a_char("abc") == ["?bc", "a?c", "ab?"]
  end

  test "Year 2018, Day 2" do
    assert Solution.run() == {7221, "mkcdflathzwsvjxrevymbdpoq"}
  end
end
