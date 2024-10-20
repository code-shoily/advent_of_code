defmodule AdventOfCode.Y2020.Day09Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2009

  alias AdventOfCode.Y2020.Day09, as: Solution

  test "Year 2020, Day 9" do
    assert Solution.run() == {15_353_384, 2_466_556}
  end
end
