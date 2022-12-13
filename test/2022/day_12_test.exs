defmodule AdventOfCode.Y2022.Day12Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2212

  alias AdventOfCode.Y2022.Day12, as: Solution

  test "Year 2022, Day 12 run/1" do
    assert Solution.run() == {528, 522}
  end
end
