# Brief - Capture Your Thoughts Effortlessly 📝

![](https://github.com/Lucasmercado101/Brief/blob/main/banner.png)

**Brief** is an intuitive, offline-first personal note-taking Progressive Web App (PWA) designed to help you capture and organize your thoughts, tasks, and ideas. It seamlessly syncs your notes across all your devices whenever you have an internet connection, ensuring you never miss a beat. Inspired by [Google Keep](https://keep.google.com/).

  

## Table of Contents 📚

- [Introduction](#introduction)

- [Features](#features)

- [Technologies Used](#technologies-used)

- [Getting Started](#getting-started)

  - [Prerequisites](#prerequisites)

  - [Setting Up the Project](#setting-up-the-project)

- [How It Works](#how-it-works)

- [Contributing](#contributing)

- [License](#license)

- [Acknowledgments](#acknowledgments)

  

## Introduction 🌟

  

Welcome to **Brief** - a modern, offline-first note-taking app tailored for your personal and professional life. Our progressive web app (PWA) is all about helping you jot down and organize your ideas, tasks, and notes seamlessly. Whether you're offline or online, Brief ensures your notes are just a tap away, ready to be synced whenever you're connected to the internet. This open-source project embraces the AGPL-3.0 license, inviting a vibrant community of users and developers to use, enhance, and customize it for free.
  

## Features ✨

  

- **Offline Capabilities**: Take and manage notes anytime, anywhere.

- **Note Operations**: Add, edit, delete, and rearrange notes effortlessly.

- **Label Management**: Customize and manage labels for better organization.

- **Sync**: A smart command API pattern ensures your data remains up-to-date across all your devices.


- [ ] **Progressive Web App**: Enhancing the app to work as a Progressive Web App.

- [ ] **Database Syncing**: Future capabilities include syncing IndexedDB with the server for a seamless offline experience.
- [ ] **Real-time Editing**: Soon, you'll be able to collaborate with others in real-time with Operational Transactions.
  

## Technologies Used 🛠️

  

- **Frontend**: Crafted using [Elm](https://elm-lang.org/), featuring Material Design icons.

- **Backend**: Powered by [ElysiaJS](https://elysiajs.com/), with a [Prisma.js](https://www.prisma.io/) ORM and PostgreSQL database.

- **Sync Strategy**: Utilizing an "Offline Queue" and "Last Write Wins" approach to resolve conflicts.

- **Session Management**: Utilizes signed cookie-based sessions to enhance security and maintain a reliable user experience.

  

## How It Works 🔄


Brief implements a Command API pattern to intelligently manage your actions—like creating, updating, or deleting notes and labels—by queuing them up until an internet connection is available. All local changes are securely stored and then synchronized with the server, which serves as the Single Source of Truth (SSOT). This ensures that your data is consistent across all devices and sessions once synced.


  

## Getting Started 🚀

  

### Prerequisites

  

To set up your local environment for Brief, you will need Docker and Bun (a JavaScript runtime).

  

### Setting Up the Project

  

1. Clone the repository:

```

git clone https://github.com/Lucasmercado101/Brief

```

2. **Start a Docker PostgreSQL instance**:

```

docker run --name notes-db -d -e POSTGRES_DB=mydb -e POSTGRES_PASSWORD=testpass123 -e POSTGRES_USER=postgres -p "6500:5432" postgres

```

3. **Access the Database with PSQL**:

```

psql -U postgres -d mydb

```

4. **Frontend Setup**:

```

npm install

npm run dev

```

  Serve the HTML or use `elm reactor` and access the app at `http://localhost:8000/frontend/public/index.html`.

  

5. **Backend Setup**:

```

bun install

bun prisma generate

bun dev

```

  

6. **Environment Variables**:

Set up your `.env` file with the following entries:

```

DATABASE_URL="postgresql://postgres:testpass123@localhost:6500/mydb"

COOKIE_SECRETS="secret"

```

  

## License 📝

  

This project is licensed under the AGPL-3.0 License - see the [LICENSE](LICENSE) file for details.

