defmodule AdventOfCode.Y2022.Day16Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2216

  alias AdventOfCode.Y2022.Day16, as: Solution

  test "Year 2022, Day 16 run/1" do
    assert Solution.run() == {1673, 2343}
  end
end
