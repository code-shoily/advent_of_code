defmodule AdventOfCode.Y2019.Day5Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2019.Day5, as: Solution

  test "Year 2019, Day 5, Part 1" do
    assert Solution.run_1() == 6_745_903
  end

  test "Year 2019, Day 5, Part 2" do
    assert Solution.run_2() == :not_implemented
  end
end
