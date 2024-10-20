defmodule AdventOfCode.Y2023.Day15Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2315

  alias AdventOfCode.Y2023.Day15, as: Solution

  test "Year 2023, Day 15 run/1" do
    assert Solution.run() == {510_273, 212_449}
  end
end
