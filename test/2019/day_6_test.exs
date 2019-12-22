defmodule AdventOfCode.Y2019.Day6Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2019.Day6, as: Solution

  test "Year 2019, Day 6, Part 1" do
    assert Solution.run_1() == 147_807
  end

  test "Year 2019, Day 6, Part 2" do
    assert Solution.run_2() == 229
  end
end
