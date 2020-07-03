defmodule AdventOfCode.Y2018.Day5Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y185

  alias AdventOfCode.Y2018.Day5, as: Solution

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
