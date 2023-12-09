defmodule AdventOfCode.Y2023.Day09Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2309

  alias AdventOfCode.Y2023.Day09, as: Solution

  test "Year 2023, Day 9 run/1" do
    assert Solution.run() == {1_939_607_039, 1041}
  end
end
