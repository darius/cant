(import (use "lib/format-tables") format-table tabulate)

(let eg ("I love bees don't they enthrall you too? Bees! BEEEZE." .split))

(each! print (format-table (tabulate eg 30)))
