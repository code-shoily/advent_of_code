defmodule AdventOfCode.Y2022.Day07Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2207

  alias AdventOfCode.Y2022.Day07, as: Solution

  test "Year 2022, Day 7 run/1" do
    assert Solution.run() == {1_297_159, 3_866_390}
  end
end
