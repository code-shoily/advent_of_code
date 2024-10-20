defmodule AdventOfCode.Y2017.Day02Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1702

  alias AdventOfCode.Y2017.Day02, as: Solution

  test "Year 2017, Day 2" do
    assert Solution.run() == {32_020, 236}
  end
end
