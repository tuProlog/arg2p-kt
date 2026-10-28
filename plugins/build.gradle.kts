plugins {
    base
}

// The plugins of Arg2P for AI assistants (see README.md): their skills, and the MCP server they run. A release attaches
// them as zips, for the clients that take them uploaded: a skill each (its folder at the root, as claude.ai asks), and
// the whole plugin.

val skills = layout.projectDirectory.dir("arg2p/skills")

val skillPackages =
    skills.asFile
        .listFiles { file -> file.resolve("SKILL.md").isFile }
        .orEmpty()
        .sortedBy { it.name }
        .map { skill ->
            tasks.register<Zip>("${skill.name}Package") {
                group = "plugins"
                description = "Packs the skill ${skill.name}, for the clients that take skills uploaded"
                archiveFileName.set("${skill.name}-skill-${project.version}.zip")
                from(skill) { into(skill.name) }
            }
        }

val pluginPackage by tasks.registering(Zip::class) {
    group = "plugins"
    description = "Packs the plugin arg2p (skills and MCP server), for the clients that take plugins uploaded"
    archiveFileName.set("arg2p-plugin-${project.version}.zip")
    from(layout.projectDirectory.dir("arg2p")) { into("arg2p") }
}

tasks.register("pluginPackages") {
    group = "plugins"
    description = "Packs the skills and the plugin of Arg2P (build/distributions)"
    dependsOn(skillPackages, pluginPackage)
}
