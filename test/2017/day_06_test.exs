defmodule AdventOfCode.Y2017.Day06Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1706

  alias AdventOfCode.Y2017.Day06, as: Solution

  test "Year 2017, Day 6 run/1" do
    assert Solution.run() == {11_137, 1037}
  end
end
