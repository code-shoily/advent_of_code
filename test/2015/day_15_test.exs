defmodule AdventOfCode.Y2015.Day15Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1515

  alias AdventOfCode.Y2015.Day15, as: Solution

  test "Year 2015, Day 15" do
    assert Solution.run() == {13_882_464, 11_171_160}
  end
end
