defmodule AdventOfCode.Y2022.Day06Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2206

  alias AdventOfCode.Y2022.Day06, as: Solution

  test "Year 2022, Day 6 run/1" do
    assert Solution.run() == {1651, 3837}
  end
end
