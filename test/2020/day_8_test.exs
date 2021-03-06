defmodule AdventOfCode.Y2020.Day8Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y208

  alias AdventOfCode.Y2020.Day8, as: Solution

  test "Year 2020, Day 8, Part 1" do
    assert Solution.run_1() == 2080
  end

  test "Year 2020, Day 8, Part 2" do
    assert Solution.run_2() == 2477
  end
end
