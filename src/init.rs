use std::fs;
use std::io::{self, Write};
use std::process::Command;

pub fn init_project(project_name: &str) {
    println!("Initializing project: {}", project_name);

    let src_path = format!("{}/src", project_name);
    let main_file_path = format!("{}/main.lln", src_path);

    if fs::create_dir_all(&src_path).is_ok() {
        fs::write(&main_file_path, "// Entry point\nfunc Main() {\n    println(\"hello world\");\n}\nMain();").unwrap();
        println!("✅ Created {}", main_file_path);
    } else {
        eprintln!("❌ Failed to create project structure.");
        return;
    }

    print!("Would you like to use Ninjar V1.9.6 for executable support? (Y/N): ");
    io::stdout().flush().unwrap();

    let mut response = String::new();
    io::stdin().read_line(&mut response).unwrap();  

    if response.trim().eq_ignore_ascii_case("y") {
        println!("📦 Cloning Ninjar...");

        let status = Command::new("git")
            .args([
                "clone",
                "--branch",
                "v1.9.6",
                "https://github.com/mebaadwaheed/ninjar",
                &format!("{}/lib/ninjar", project_name),
            ])
            .status()
            .expect("Failed to run git clone");

        if status.success() {
            println!("✅ Ninjar installed to {}/lib/ninjar", project_name);
        } else {
            println!("❌ Failed to install Ninjar.");
        }
    }

    println!("✅ Project initialized successfully.");
}
