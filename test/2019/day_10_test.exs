defmodule AdventOfCode.Y2019.Day10Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2019.Day10, as: Solution

  test "Year 2019, Day 10, Part 1" do
    assert Solution.run_1() == 263
  end

  test "Year 2019, Day 10, Part 2" do
    assert Solution.run_2() == 1110
  end
end
