defmodule AdventOfCode.Y2020.Day14Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2014

  alias AdventOfCode.Y2020.Day14, as: Solution

  test "Year 2020, Day 14" do
    assert Solution.run() == {9_967_721_333_886, 4_355_897_790_573}
  end
end
