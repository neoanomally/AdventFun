use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::fs::File;
use std::path::{PathBuf};



pub fn read2025(child: &str) -> Vec<String> {
    let config = read_default_config();

    let data_path = config.get("advent_data")
        .expect("Error reading advent data");

    let mut path = PathBuf::new();
    path.push(data_path);
    path.push("twentyfive");
    path.push(child);


    let mut buffer: Vec<String> = Vec::new();
    println!("Reading in Path: {}", path.to_str().unwrap());
    let file = File::open(&path).unwrap();
    let reader = BufReader::new(file);


    for line in reader.lines() {
        let line = line.expect("Error reading line for 2025");
        buffer.push(line);
    }

    buffer
}


pub fn read_default_config() -> HashMap<String, String> {

    let content = std::fs::read_to_string("config/default.toml")
        .expect("error reading default.toml");

    parse_simple_config(&content)
}

pub fn parse_simple_config(content: &str) -> HashMap<String, String> {
    let mut config = HashMap::new();

    for line in content.lines() {
        let line = line.trim();

        if line.is_empty() || line.starts_with("#") {
            continue;
        }

        if let Some((key, value)) = line.split_once("=") {
            let key = key.trim().to_string();
            let mut value = value.trim().to_string();

            if (value.starts_with('"') && value.ends_with('"')) || 
                (value.starts_with('\'') || value.ends_with('\'')) {
                    value = value[1..value.len() - 1].to_string();
            } else {
                value = value.to_string();
            };
            

            config.insert(key, value);
        }
    }

    config
}
