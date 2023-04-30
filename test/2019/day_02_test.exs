defmodule AdventOfCode.Y2019.Day02Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1902

  alias AdventOfCode.Y2019.Day02, as: Solution

  test "Year 2019, Day 2" do
    assert Solution.run() == {3_562_624, 8298}
  end
end
