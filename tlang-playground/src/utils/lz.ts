const lzString = () => import('lz-string');

export async function compressSource(source: string) {
  let { compressToEncodedURIComponent } = await lzString();

  return compressToEncodedURIComponent(source);
}

export async function decompressSource(source: string) {
  let { decompressFromEncodedURIComponent } = await lzString();

  return decompressFromEncodedURIComponent(source);
}
