defmodule AdventOfCode.Y2022.Day18Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2218

  alias AdventOfCode.Y2022.Day18, as: Solution

  test "Year 2022, Day 18 run/1" do
    assert Solution.run() == {3650, 2118}
  end
end
