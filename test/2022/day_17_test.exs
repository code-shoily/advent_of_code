defmodule AdventOfCode.Y2022.Day17Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2217

  alias AdventOfCode.Y2022.Day17, as: Solution

  test "Year 2022, Day 17 run/1" do
    assert Solution.run() == {3209, 1_580_758_017_509}
  end
end
