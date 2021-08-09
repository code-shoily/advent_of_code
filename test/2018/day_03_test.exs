defmodule AdventOfCode.Y2018.Day03Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1803

  alias AdventOfCode.Y2018.Day03, as: Solution

  test "Input processor" do
    sample = """
    #1 @ 1,3: 4x4
    #2 @ 3,1: 4x4
    #3 @ 5,5: 2x2
    """

    output = [
      %{id: 1, dimension: {4, 4}, origin: {1, 3}},
      %{id: 2, dimension: {4, 4}, origin: {3, 1}},
      %{id: 3, dimension: {2, 2}, origin: {5, 5}}
    ]

    assert Solution.process(sample) == output
  end

  test "Year 2018, Day 3, Part 1" do
    assert Solution.run_1() == 110_389
  end

  test "Year 2018, Day 3, Part 2" do
    assert Solution.run_2() == 552
  end
end
