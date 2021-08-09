defmodule AdventOfCode.Y2018.Day05Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1805

  alias AdventOfCode.Y2018.Day05, as: Solution

  test "React (1)" do
    assert Solution.react('dabAcCaCBAcCcaDA') == 10
  end

  test "Year 2018, Day 5, Part 1" do
    assert Solution.run_1() == 10_496
  end

  test "Year 2018, Day 5, Part 2" do
    assert Solution.run_2() == 5774
  end
end
