defmodule AdventOfCode.Y2023.Day14Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2314

  alias AdventOfCode.Y2023.Day14, as: Solution

  test "Year 2023, Day 14 run/1" do
    assert Solution.run() == {110_565, 89_845}
  end
end
