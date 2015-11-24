
Make you able to inspect SMPP PDU

## Build

```bash
make
```

## Run

### Help message

```bash
./parse_pdu
```

### Parse Pdu that contains date time preffix and `:` delimiters

```bash
./parse_pdu "<HexWithDelimeters>"
```

### Parse plain SMPP PDU HEX

```bash
./parse_pdu plain "<Hex>"
```

Example output:

```bash
./parse_pdu plain "000000450000000400000000000003F700010174747400010133373532393635333935353500000001003030303030323030303030303030305200000000000568656C6C6F"
Hex: "000000450000000400000000000003F700010174747400010133373532393635333935353500000001003030303030323030303030303030305200000000000568656C6C6F"
Params: [{'short_message.hex',"68656C6C6F"},
         {command_id,4},
         {command_status,0},
         {sequence_number,1015},
         {short_message,"hello"},
         {sm_default_msg_id,0},
         {data_coding,0},
         {replace_if_present_flag,0},
         {registered_delivery,0},
         {validity_period,"000002000000000R"},
         {schedule_delivery_time,[]},
         {priority_flag,1},
         {protocol_id,0},
         {esm_class,0},
         {destination_addr,"375296539555"},
         {dest_addr_npi,1},
         {dest_addr_ton,1},
         {source_addr,"ttt"},
         {source_addr_npi,1},
         {source_addr_ton,1},
         {service_type,[]}]
```
