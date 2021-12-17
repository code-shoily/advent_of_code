defmodule AdventOfCode.Y2021.Day16Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2116

  alias AdventOfCode.Y2021.Day16, as: Solution

  test "Year 2021, Day 16, Part 1" do
    assert Solution.run_1() == 879
  end

  test "Year 2021, Day 16, Part 2" do
    assert Solution.run_2() == 539_051_801_941
  end
end
