import Fastify from 'fastify';
import fastifyStatic from 'fastify-static';
import path from 'path';
import { promisify } from 'util';
import { readFile } from 'fs';

const readFileAsync = promisify(readFile);

const dictionaryPromise = readFileAsync('./words.txt', { encoding: 'utf-8' });

const app = Fastify({
  logger: true
});

app.register(fastifyStatic, {
  root: path.join(__dirname, '../../client/dist'),
  prefix: '/client/dist',
});

app.get('/', (req, res) => {
  res.sendFile('index.html');
});

app.get('/words', async () => {
  const dictionary = await dictionaryPromise;
  return dictionary
    .split('\n')
    .filter((word) => word.length === 5)
    .map((word) => word.toLowerCase());
});

app.get('/word', async () => {
  const dictionary = await dictionaryPromise;
  const processedDictionary = dictionary
    .split('\n')
    .filter((word) => word.length === 5)
    .map((word) => word.toLowerCase());

  const random = Math.floor(Math.random() * (processedDictionary.length - 1));
  return processedDictionary[random];
});

app.listen(3000, (_, err) => {
  app.log.info('Server is running on port 3000');
  if (err) {
    app.log.error(err);
  }
});
