defmodule AdventOfCode.Y2023.Day13Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2313

  alias AdventOfCode.Y2023.Day13, as: Solution

  test "Year 2023, Day 13 run/1" do
    assert Solution.run() == {33_047, 28_806}
  end
end
