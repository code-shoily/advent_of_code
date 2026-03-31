defmodule AdventOfCode.Y2019.Day18Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1918

  alias AdventOfCode.Y2019.Day18, as: Solution

  test "Year 2019, Day 18 run/1" do
    assert Solution.run() == {6098, 1698}
  end
end
