defmodule AdventOfCode.Y2022.Day14Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2214

  alias AdventOfCode.Y2022.Day14, as: Solution

  test "Year 2022, Day 14 run/1" do
    assert Solution.run() == {793, 24_166}
  end
end
