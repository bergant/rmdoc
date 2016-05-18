select
	tabs.name,
	sep.value as table_desc,
	sep.major_id as id
from
  sys.tables tabs
  left join sys.extended_properties sep on
		sep.major_id = tabs.object_id
		and sep.minor_id = 0
		and sep.name = 'MS_Description'
