defmodule AdventOfCode.Y2015.Day02Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1502

  alias AdventOfCode.Y2015.Day02, as: Solution

  test "Year 2015, Day 2" do
    assert Solution.run() == {1_606_483, 3_842_356}
  end
end
