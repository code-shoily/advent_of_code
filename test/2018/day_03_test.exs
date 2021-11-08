defmodule AdventOfCode.Y2018.Day03Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1803

  alias AdventOfCode.Y2018.Day03, as: Solution

  test "Year 2018, Day 3, Part 1" do
    assert Solution.run_1() == 110_389
  end

  test "Year 2018, Day 3, Part 2" do
    assert Solution.run_2() == 552
  end
end
