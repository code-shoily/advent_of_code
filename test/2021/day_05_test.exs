defmodule AdventOfCode.Y2021.Day05Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2105

  alias AdventOfCode.Y2021.Day05, as: Solution

  test "Year 2021, Day 5" do
    assert Solution.run() == {4655, 20_500}
  end
end
