defmodule AdventOfCode.Y2017.Day09Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1709

  alias AdventOfCode.Y2017.Day09, as: Solution

  test "Year 2017, Day 9" do
    assert Solution.run() == {7616, 3838}
  end
end
