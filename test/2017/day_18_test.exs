defmodule AdventOfCode.Y2017.Day18Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1718

  alias AdventOfCode.Y2017.Day18, as: Solution

  test "Year 2017, Day 18 run/1" do
    assert Solution.run() == {3188, {:todo, 2}}
  end
end
