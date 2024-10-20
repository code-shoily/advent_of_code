defmodule AdventOfCode.Y2022.Day05Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2205

  alias AdventOfCode.Y2022.Day05, as: Solution

  test "Year 2022, Day 5 run/1" do
    assert Solution.run() == {"VPCDMSLWJ", "TPWCGNCCG"}
  end
end
