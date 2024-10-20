defmodule AdventOfCode.Y2017.Day12Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1712

  alias AdventOfCode.Y2017.Day12, as: Solution

  test "Year 2017, Day 12 run/1" do
    assert Solution.run() == {239, 215}
  end
end
